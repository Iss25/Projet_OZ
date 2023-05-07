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
   Window
   NGram = 4
   %%% Pour ouvrir les fichiers
   class TextFile
      from Open.file Open.text
   end

   proc {Browse Buf}
      {Browser.browse Buf}
   end

   NbThreads = 500

   %%% -------------   TODO ---------------------

   %%% /!\ Fonction testee /!\
   %%% @pre : les threads sont "ready"
   %%% @post: Fonction appellee lorsqu on appuie sur le bouton de prediction
   %%%        Affiche la prediction la plus probable du prochain mot selon les deux derniers mots entres
   %%% @return: Retourne une liste contenant la liste du/des mot(s) le(s) plus probable(s) accompagnee de 
   %%%          la probabilite/frequence la plus elevee. 
   %%%          La valeur de retour doit prendre la forme:
   %%%                  <return_val> := <most_probable_words> '|' <probability/frequence> '|' nil
   %%%                  <most_probable_words> := <atom> '|' <most_probable_words> 
   %%%                                           | nil
   %%%                  <probability/frequence> := <int> | <float>

   %%% A QUOI ÇA SERT ??! (à lire avec la voix de Deville) 
   
   %%% -------------   TODO ---------------------




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
      nil then Return in
         if BestPrediction == '' then Return = [[nil] BestFrequency]
         else 
            Return = [[BestPrediction] [{Int.toFloat BestFrequency}]]
         end
      [] H|T then
         if Tree.H > BestFrequency then {GetBestPrediction Tree T H Tree.H}
         elseif Tree.H == BestFrequency then {GetBestPrediction Tree T {VirtualString.toString (H#' '#BestPrediction)} BestFrequency}  
         else 
            {GetBestPrediction Tree T BestPrediction BestFrequency}
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
            {ReadStream T {UpdateOutputTree H {Arity H} Tree}}
         end
      end
   end

   fun {SortedTree SortedArity Tree NewTree N} 
      if N == 0 then NewTree 
      else
         case SortedArity 
         of nil then NewTree
         [] H|T then Prediction = prediction(H:Tree.H) in
            {SortedTree T Tree {Adjoin NewTree Prediction} N-1}
         end
      end
   end

   fun {TreeToArray Tree}
      fun {TreeToArrayAcc Arity Tree Acc}
         case Arity
         of nil then Acc
         [] H|T then Value = {VirtualString.toString H#":"#({Int.toFloat Tree.H})} in
            {Browse {List.map Acc String.toAtom}}
            if {Length Acc} == 0 then {TreeToArrayAcc T Tree [Value]}
            else {TreeToArrayAcc T Tree {List.append Acc [Value]}}
            end
         end
      end
   in
      {TreeToArrayAcc {SortArity Tree} Tree nil}
   end

   fun {SortArity Tree}
      {List.sort {Arity Tree} fun{$ A B} Tree.A > Tree.B end}
   end


   %%%
   %%% Function called when the prediction task is launched
   %%%

   fun {Press}
      local PredictionTree TempPredictionTree BestPrediction SeparatedWordsStream SeparatedWordsPort Return ATree NPredictionsTree NPredictionsArray in 
         {OutputText set("Loading... Please wait")}

         SeparatedWordsPort = {NewPort SeparatedWordsStream}
         
         {LaunchThreads SeparatedWordsPort NbThreads}
         TempPredictionTree = prediction()
         PredictionTree = {ReadStream SeparatedWordsStream TempPredictionTree}
         ATree = prediction()
         NPredictionsTree = {SortedTree {SortArity PredictionTree} PredictionTree ATree 4}
         NPredictionsArray = {List.map {TreeToArray NPredictionsTree} String.toAtom}
         {System.show NPredictionsArray}
         %BestPrediction = {GetBestPrediction PredictionTree {Arity PredictionTree} '' 0}
         if {Length NPredictionsArray} == 0 then Return = "Not Found" 
         else 
            Return = NPredictionsArray %{Value.toVirtualString BestPrediction} 
         end
         {OutputText set(Return)}
         {Browse NPredictionsTree}
         NPredictionsTree
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

   fun {ParseLine Line InputTextSplit Found}
      case Line#InputTextSplit 
      of nil#nil then nil
      [] (A|B)#(C|D) then if A == C then {ParseLine B D true} else {ParseLine B InputTextSplit false} end
      [] (H|T)#nil then if Found then H else nil end
      [] nil#(H|T) then nil
      else nil
      end
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
   %%% Check if the character is not present in the given List
   %%%      Char:      Character to check presence
   %%%      MatchList: List of characters to check equality with Char
   %%%
   %%% Returns false if Char is not present in Matchlist, true otherwise
   %%% 

   fun {DoesntMatch Char MatchList}
      case MatchList of 
      nil then true
      [] H|T then
         if H.1 == Char then false
         else
            {DoesntMatch Char T} 
         end 
      end 
   end

   fun {DoesntMatch2 Char MatchList}
      case MatchList of 
      nil then false %true
      [] H|T then
         if H.1 == Char then true %false
         else
            {DoesntMatch2 Char T} 
         end 
      end 
   end

   fun {SpecialToSpace List MatchList ResList}
      case List of
      nil then ResList
      [] H|T then
         if {DoesntMatch2 H MatchList} then {SpecialToSpace T MatchList H|ResList}
         else 
            {SpecialToSpace T MatchList 32|ResList}
         end
      end 
   end

   fun{SpecialToSpace2 List}
      case List of
      nil then nil
      [] H|T then 
         if H \= 48 then 32|{SpecialToSpace2 T}
         elseif H \= 49 then 32|{SpecialToSpace2 T}
         elseif H \= 50 then 32|{SpecialToSpace2 T}
         elseif H \= 51 then 32|{SpecialToSpace2 T}
         elseif H \= 52 then 32|{SpecialToSpace2 T}
         elseif H \= 53 then 32|{SpecialToSpace2 T}
         elseif H \= 54 then 32|{SpecialToSpace2 T}
         elseif H \= 55 then 32|{SpecialToSpace2 T}
         elseif H \= 56 then 32|{SpecialToSpace2 T}
         elseif H \= 57 then 32|{SpecialToSpace2 T}
         elseif H \= 97 then 32|{SpecialToSpace2 T}
         elseif H \= 98 then 32|{SpecialToSpace2 T}
         elseif H \= 99 then 32|{SpecialToSpace2 T}
         elseif H \= 100 then 32|{SpecialToSpace2 T}
         elseif H \= 101 then 32|{SpecialToSpace2 T}
         elseif H \= 102 then 32|{SpecialToSpace2 T}
         elseif H \= 103 then 32|{SpecialToSpace2 T}
         elseif H \= 104 then 32|{SpecialToSpace2 T}
         elseif H \= 105 then 32|{SpecialToSpace2 T}
         elseif H \= 106 then 32|{SpecialToSpace2 T}
         elseif H \= 107 then 32|{SpecialToSpace2 T}
         elseif H \= 108 then 32|{SpecialToSpace2 T}
         elseif H \= 109 then 32|{SpecialToSpace2 T}
         elseif H \= 110 then 32|{SpecialToSpace2 T}
         elseif H \= 111 then 32|{SpecialToSpace2 T}
         elseif H \= 112 then 32|{SpecialToSpace2 T}
         elseif H \= 113 then 32|{SpecialToSpace2 T}
         elseif H \= 114 then 32|{SpecialToSpace2 T}
         elseif H \= 115 then 32|{SpecialToSpace2 T}
         elseif H \= 116 then 32|{SpecialToSpace2 T}
         elseif H \= 117 then 32|{SpecialToSpace2 T}
         elseif H \= 118 then 32|{SpecialToSpace2 T}
         elseif H \= 119 then 32|{SpecialToSpace2 T}
         elseif H \= 120 then 32|{SpecialToSpace2 T}
         elseif H \= 121 then 32|{SpecialToSpace2 T}
         elseif H \= 122 then 32|{SpecialToSpace2 T}
         else H|{SpecialToSpace2 T}
         end
      end
   end

   fun{SpecialToSpace3 MatchList Char}
      case MatchList of
      nil then Char
      [] H|T then 
         if H == Char then {String.replace Char H 32}
         else {SpecialToSpace3 T Char}
         end
      end
   end
   %%%
   %%% Strips ponctuation symbols from given String
   %%%      Str: String to strip ponctuation from
   %%%
   %%% Returns truncated String
   %%%

   fun {StripPonctuation Str}
      local Ponctuation Alphabet Res in 
         Ponctuation = ["," "." ":" "'" "-" "!" "?" ";" "_"]
         Alphabet = ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" " "]
         %{List.filter Str fun {$ Char} {DoesntMatch Char Ponctuation} end}
         %Res = nil
         %{SpecialToSpace Str Alphabet Res}
         %{SpecialToSpace2 Str}
         {SpecialToSpace3 Ponctuation Str}
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
         Prediction = {ParseLine {List.map {String.tokens {StripPonctuation Line} & } Lower} InputTextSplit false}
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
      local Tree FPT Content Xni Y Z in 
         Tree = tree()
         if First then FPT = FilePerThread + {Length Files} mod N else FPT = FilePerThread end
         thread 
            local R in 
               {List.takeDrop Files FPT Y Z}
               R = {LaunchTask Y Tree Input} 
               {Send Port R}
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
         {InputText set({StripLastChar Content})}
         Input = {NgramInput {List.map {String.tokens {StripLastChar Content} & } Lower}}
         {Browse {List.map Input String.toAtom}}
         {LaunchThread Input Port true N Xn Files FilePerThread}
      end
   end

   %%% Fetch Tweets Folder from CLI Arguments
   %%% See the Makefile for an example of how it is called
   fun {GetSentenceFolder}
      Args = {Application.getArgs record('folder'(single type:string optional:false))}
   in
      {Property.condGet 'directory' Args.'folder'}
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
    
   proc {Main}
      %% Fonction d'exemple qui liste tous les fichiers
      %% contenus dans le dossier passe en Argument.
      %% Inspirez vous en pour lire le contenu des fichiers
      %% se trouvant dans le dossier
      %%% N'appelez PAS cette fonction lors de la phase de
      %%% soumission !!!
      % {ListAllFiles {OS.getDir TweetsFolder}}
      
      local NbThreads Description S in
         {Property.put print foo(width:1000 depth:1000)}  
         % Creation de l interface graphique
         Description=td(
            title: "Text predictor"
            % winfo(height:PH)
            lr(
               text(handle:InputText width:50 height:10 background:white foreground:black wrap:word) 
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
            text(handle:OutputText width:50 height:10 background:black foreground:white glue:nw wrap:word)
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
      end
   end
   {Main}
end