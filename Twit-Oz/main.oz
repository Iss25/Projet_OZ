functor
import 
   QTk at 'x-oz://system/wp/QTk.ozf'
   Application
   Open
   OS
   Property
   Browser
define
   InputText 
   OutputText
   NGram = 2
   %%% Pour ouvrir les fichiers
   class TextFile
      from Open.file Open.text
   end

   proc {Browse Buf}
      {Browser.browse Buf}
   end

   NbThreads = 500

   %%%
   %%% Computes best prediction based on the pairs of prediction-frequency present in the record
   %%%      Tree:           Record to read
   %%%      Arity:          Remaining to read record keys 
   %%%      BestPrediction: Current best computed prediction
   %%%      BestFrequency:  Current best computed prediction's frequency
   %%%
   %%% Returns the best prediction based on its frequency
   %%%

   fun {GetBestPrediction Tree Arity BestPrediction BestFrequency}
      case Arity of 
      nil then [BestPrediction BestFrequency]
      [] H|T then
         if Tree.H > BestFrequency then {GetBestPrediction Tree T [H] Tree.H}
         elseif Tree.H == BestFrequency then {GetBestPrediction Tree T {List.append BestPrediction [H]} BestFrequency}
         else {GetBestPrediction Tree T BestPrediction BestFrequency}
         end
      end
   end


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
            local Value Val PredictionTree in 
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
         else {ReadStream T {UpdateOutputTree H {Arity H} Tree}} end
      end
   end


   %%%
   %%% Transform a List into a character chain
   %%%      List: List to transform
   %%%
   %%% Returns a character chain
   %%%

   fun {ListToString List}
      fun {ListToStringAcc List Acc}
         case List of
         nil then Acc
         [] H|T then
            {ListToStringAcc T {VirtualString.toAtom Acc#'|'#H}}
         end
      end
   in
      {ListToStringAcc List nil}
   end


   %%%
   %%% Function called when the prediction task is launched
   %%%

   fun {Press}
      local PredictionTree TempPredictionTree BestPrediction SeparatedWordsStream SeparatedWordsPort Return in 
         {InputText set(state:disabled)}
         {OutputText set(state:normal)}
         {OutputText set("Loading... Please wait")}
         {OutputText set(state:disabled)}

         SeparatedWordsPort = {NewPort SeparatedWordsStream}
         
         {LaunchThreads SeparatedWordsPort NbThreads}
         TempPredictionTree = prediction()
         PredictionTree = {ReadStream SeparatedWordsStream TempPredictionTree}
         BestPrediction = {GetBestPrediction PredictionTree {Arity PredictionTree} [nil] 0}
         if BestPrediction.2 == [0] then Return = "Not Found" 
         else 
            Return = {VirtualString.toAtom {ListToString BestPrediction.1}#' -> '#{IntToFloat (BestPrediction.2).1}}
         end

         {OutputText set(state:normal)}
         {OutputText set(Return)}
         {OutputText set(state:disabled)}
         {InputText set(state:normal)}
         BestPrediction
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
      fun {ParseLineA CurrentLine CurrentInputTextSplit InitialLength CurrentLength Struct}
         case CurrentLine#CurrentInputTextSplit 
         of nil#nil then Struct
         [] (A|B)#(C|D) then if A == C then 
            {ParseLineA B D InitialLength CurrentLength+1 Struct} else {ParseLineA B InputTextSplit InitialLength 0 Struct} end
         [] (H|T)#nil then 
            if CurrentLength == InitialLength then {ParseLineA T InputTextSplit InitialLength 0 {UpdatePredictionTree Struct H}}
            else {ParseLineA T InputTextSplit {Length InputTextSplit} 0 Struct} 
            end
         [] nil#(_|_) then Struct
         else Struct
         end
      end
      A T
   in
      T = tree()
      A = {ParseLineA Line InputTextSplit {Length InputTextSplit} 0 T}
      A  
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
         if H == nil then {SpecialToSpace T ResList}
         elseif H == 32 then {SpecialToSpace T H|ResList}
         elseif H == 39 then {SpecialToSpace T ResList}
         elseif H == 10 then {SpecialToSpace T 32|ResList}
         elseif H == 47 then {SpecialToSpace T 32|ResList}
         elseif H < 48 then {SpecialToSpace T ResList}
         elseif H > 122 then {SpecialToSpace T ResList}
         else 
            if H > 96 then {SpecialToSpace T H|ResList}
            elseif H < 58 then {SpecialToSpace T H|ResList}
            else 
               {SpecialToSpace T ResList}
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
   %%% Take a String and replace consecutive space to one single space
   %%%      String: String to replace consecutive space from
   %%%
   %%% Returns updated String
   %%%

   fun {FilterDoubleSpace String}
      fun {FilterDoubleSpaceBool A LastSpace}
         case A 
         of nil then nil
         [] H|T then 
            if H == 32 then
               if LastSpace then
                  {FilterDoubleSpaceBool T true}
               else
                  32|{FilterDoubleSpaceBool T true}
               end
            else
               H|{FilterDoubleSpaceBool T false}
            end
         end
      end
   in
      {FilterDoubleSpaceBool String false}
   end


   %%%
   %%% Parses a file a computes a record of prediction mapped with frequency after input in the given file
   %%%      File:           File to parse
   %%%      Line:           Currently parsed line
   %%%      Struct:         Up to now computed prediction record for the file
   %%%      InputTextSplit: Array of words from user input
   %%% Returns computed prediction record
   %%%

   fun {ParseFile File Struct InputTextSplit} 
      local StringedFile in 
         {File read(list:StringedFile size:all)}
         {ParseLine {String.tokens {FilterDoubleSpace {StripPonctuation {Lower StringedFile}}} & } InputTextSplit}
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

   proc {LaunchTask Port Files StartIndex EndIndex CurrentIndex InputTextSplit}
      case Files of nil then skip
      [] H|T then
         if CurrentIndex >= EndIndex then skip
         elseif CurrentIndex < StartIndex then {LaunchTask Port T StartIndex EndIndex CurrentIndex+1 InputTextSplit}
         else Path Output File Tree in 
            Path = {VirtualString.toAtom {GetSentenceFolder}#"/"#H}
            File = {New TextFile init(name:Path flags:[read])}
            Tree = tree()
            Output = {ParseFile File Tree InputTextSplit}
            if {Length {Arity Output}} > 0 then {Send Port Output} end
            {LaunchTask Port T StartIndex EndIndex CurrentIndex+1 InputTextSplit}
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
      if {Length InputTextSplit} =< NGram then InputTextSplit
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
      local FPT Xni in 
         if First then FPT = FilePerThread + {Length Files} mod N else FPT = FilePerThread end
         thread 
            {LaunchTask Port Files N*FilePerThread N*FilePerThread+FPT 0 Input} 
            Xni = Xn
         end
         if N > 0 then
            {LaunchThread Input Port false N-1 Xni Files FilePerThread}
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
         Input = {NgramInput {List.map {String.tokens {FilterDoubleSpace {StripPonctuation {StripLastChar Content}}} & } Lower}}
         {LaunchThread Input Port true N Xn Files FilePerThread}
      end
   end
   

   %%% Fetch Tweets Folder from CLI Arguments
   %%% See the Makefile for an example of how it is called
   fun {GetSentenceFolder}
      Args = {Application.getArgs record('folder'(single type:string optional:false))}
   in
      Args.'folder'
   end
    
   proc {Main}
      local Description Window in
         {Property.put print foo(width:1000 depth:1000)}  
         % Creation de l interface graphique
         Description=td(
            title: "Text predictor"
            lr(text(handle:InputText width:50 height:10 background:white foreground:black wrap:word) 
               button(text:"Predict" width:15 action:proc{$} _ in _ = {Press} end))
            text(handle:OutputText width:50 height:10 background:black foreground:white glue:nw wrap:word)
            action:proc{$}{Application.exit 0} end % quitte le programme quand la fenetre est fermee
            )
         
         % Creation de la fenetre
         Window={QTk.build Description}
         {Window show}
      
         {InputText tk(insert 'end' "Loading... Please wait.")}
         {InputText bind(event:"<Control-s>" action:proc{$} _ in _ = {Press} end)} % You can also bind events
         {InputText bind(event:"<Escape>" action:proc{$}{Application.exit 0} end)}
         {InputText bind(event:"<Return>" action:proc{$} _ in _ = {Press} end)}
      
         {InputText set(1:"")}
         {OutputText set(state:disabled)}
      end
      %%ENDOFCODE%%
   end
   {Main}
end