functor
import 
   QTk at 'x-oz://system/wp/QTk.ozf'
   System
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

   InputText 
   OutputText
   NbThreads = 16
   
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

   fun {GetBestPrediction Tree Arity BestPrediction BestFrequency}
      case Arity of nil then BestPrediction else
         if Tree.(Arity.1) > BestFrequency then {GetBestPrediction Tree Arity.2 Arity.1 Tree.(Arity.1)}  
         else {GetBestPrediction Tree Arity.2 BestPrediction BestFrequency}
         end    
      end
   end

   fun {Press}
      local Contents T SeparatedWordsStream SeparatedWordsPort Return in 
         {InputText get(Contents)}
         % On lance les threads de lecture et de parsing
         SeparatedWordsPort = {NewPort SeparatedWordsStream}
         
         {LaunchThreads SeparatedWordsPort NbThreads}
         %{Browse SeparatedWordsStream.1}
         Return = {GetBestPrediction SeparatedWordsStream.1 {Arity SeparatedWordsStream.1} '' 0}
         {Browse Return}  
         
         {OutputText set(Return)}
         {OutputText get(T)}
         0
      end
   end

   %%%
   %%%  +--------------------------------------------------------------------------------+
   %%%  |                                                                                |
   %%%  |  We use the following structure                                                |
   %%%  |                                                                                |
   %%%  |                                    you                                         |
   %%%  |                                                                                |
   %%%  |        must:0.5                  look:0.6                are:0.8               |
   %%%  |                                                                                |
   %%%  |   eat:0.7  drink:0.8       awesome:0.4  sick:0.6    beautiful:0.5 stupid:0.6   |
   %%%  |                                                                                |
   %%%  |                                                                                |
   %%%  |  We store a map (record) of each word mapped with its own probability tree     |
   %%%  |  Example : you: tree(1                                                         |
   %%%  |                      must:tree(0.7 eat:tree(0.7) drink:tree(0.8))              |
   %%%  |                      look:tree(0.6 awesome:tree(0.4) sick:tree(0.6))           |
   %%%  |                      are:tree(0.8 beautiful:tree(0.5) stupid:tree(0.6))        |
   %%%  |                 )                                                              |
   %%%  |                                                                                |
   %%%  +--------------------------------------------------------------------------------+
   %%%

   fun {Lower Word}
       {List.map Word Char.toLower}
   end

   fun {ParseLine Line InputTextSplit Found}
      case Line#InputTextSplit 
      of nil#nil then nil
      [] (A|B)#(C|D) then if A == C then {ParseLine B D true} else {ParseLine B InputTextSplit false} end
      [] (H|T)#nil then if Found then H else nil end
      [] nil#(H|T) then nil
      else nil
      end
   end

   fun {UpdatePredictTree Struct Predict}
      if {String.toAtom Predict} == '' then Struct 
      else
         local Value PredictTree NewTree in 
            Value = {CondSelect Struct {String.toAtom Predict} 0}
            PredictTree = {MakeRecord tree [{String.toAtom Predict}]}
            PredictTree.{String.toAtom Predict} = Value + 1
            {Adjoin Struct PredictTree}
         end
      end
   end 

   fun {ParseFile File Line Struct InputTextSplit} 
      local B L Predict Value PredictTree NewTree in 
         Predict = {ParseLine {List.map {String.tokens Line & } Lower} InputTextSplit false}
         NewTree = {UpdatePredictTree Struct Predict}
         {File atEnd(B)}
         if B then NewTree
         else 
            {File getS(L)} 
            {ParseFile File L NewTree InputTextSplit}
         end
      end
   end

   fun {LaunchTask Files StartIndex EndIndex CurrentIndex Struct InputTextSplit}
      if Files == nil then Struct end
      if CurrentIndex >= EndIndex then Struct end
      if CurrentIndex < StartIndex then {LaunchTask Files.2 StartIndex EndIndex CurrentIndex+1 Struct InputTextSplit}
      else Path Output File Line in 
         Path = {VirtualString.toAtom {GetSentenceFolder}#"/"#Files.1}
         File = {New TextFile init(name:Path flags:[read])}
         {File getS(Line)}
         Output = {ParseFile File Line Struct InputTextSplit}
         {LaunchTask Files.2 StartIndex EndIndex CurrentIndex+1 Output InputTextSplit}
      end
   end

   fun  {GetFromTo Original From To Current Final}
      if Current < From then {GetFromTo Original.2 From To Current+1 Final} 
      elseif Current >= To then Final 
      else {GetFromTo Original.2 From To Current+1 {Append Final [Original.1]}}
      end
   end

   fun {NgramInput InputTextSplit}
      if {Length InputTextSplit} =< NGram then InputTextSplit
      else {NgramInput InputTextSplit.2}
      end
   end
   
   fun {Join S Sep Out}
      case S of nil then Out
      [] H|T then {Join T Sep (Out|Sep|H)} 
      end
   end

   fun {Reverse S}
      fun {ReverseAcc S Acc}
         case S of nil then Acc
         [] H|T then {ReverseAcc T H|Acc}
         end
      end
   in 
      {ReverseAcc S ""}
   end

   fun {StripLastChar S NChar} 
      fun {StringFirstChar Str NFchar}
         if NFchar =< 0 then Str 
         else
            case Str of nil then nil
            [] H|T then {StringFirstChar T NFchar-1}
            end
         end
      end
   in 
      {Reverse {StringFirstChar {Reverse S} NChar}}
   end

    %%% Lance les N threads de lecture et de parsing qui liront et traiteront tous les fichiers
    %%% Les threads de parsing envoient leur resultat au port Port
   proc {LaunchThreads Port N}
      local Files FilePerThread in 
         Files = {OS.getDir {GetSentenceFolder}}
         FilePerThread = {FilesAmount Files} div N
         for I in 1..N do 
            local Tree FPT Content in 
               Tree = tree()
               if I == N-1 then FPT = FilePerThread + {FilesAmount Files} mod N else FPT = FilePerThread end
               thread 
                  local R A Input in 
                     A = {GetFromTo Files I*FilePerThread I*FilePerThread+FPT I*FilePerThread {MakeList 0}}
                     
                     %{Browse {Length A}}
                     {InputText get(Content)}
                     Input = {NgramInput {List.map {String.tokens {StripLastChar Content 2} & } Lower}}
                     {Browse {List.map Input String.toAtom}}
                     R = {LaunchTask Files I*FilePerThread I*FilePerThread+FPT 0 Tree Input} 
                     {Send Port R}
                  end 
               end
            end
         end
      end
   end
   
   %%% Ajouter vos fonctions et procédures auxiliaires ici


   %%% Fetch Tweets Folder from CLI Arguments
   %%% See the Makefile for an example of how it is called
   fun {GetSentenceFolder}
      Args = {Application.getArgs record('folder'(single type:string optional:false))}
   in
      Args.'folder'
   end

   %%% Decomnentez moi si besoin
   %proc {ListAllFiles L}
   %   case L of nil then skip
   %   [] H|T then {Browse {String.toAtom H}} {ListAllFiles T}
   %   end
   %end

   fun {FilesAmount L}
      fun {FilesAmountAcc L A}
         case L of nil then A
         [] H|T then {FilesAmountAcc T A+1} end
      end
   in 
      {FilesAmountAcc L 0}
   end
    
   %%% Procedure principale qui cree la fenetre et appelle les differentes procedures et fonctions
   proc {Main}

      TweetsFolder = {GetSentenceFolder}
   in
      %% Fonction d'exemple qui liste tous les fichiers
      %% contenus dans le dossier passe en Argument.
      %% Inspirez vous en pour lire le contenu des fichiers
      %% se trouvant dans le dossier
      %%% N'appelez PAS cette fonction lors de la phase de
      %%% soumission !!!
      % {ListAllFiles {OS.getDir TweetsFolder}}
       
      local NbThreads Description Window SeparatedWordsStream in
	 {Property.put print foo(width:1000 depth:1000)}  % for stdout siz
	 
            % TODO
	 
            % Creation de l interface graphique
	 Description=td(
			title: "Text predictor"
			lr(text(handle:InputText width:50 height:10 background:white foreground:black wrap:word) button(text:"Predict" width:15 action:proc{$} X in X = {Press} end))
			text(handle:OutputText width:50 height:10 background:black foreground:white glue:w wrap:word)
			action:proc{$}{Application.exit 0} end % quitte le programme quand la fenetre est fermee
			)
	 
            % Creation de la fenetre
	 Window={QTk.build Description}
	 {Window show}
	 
	 {InputText tk(insert 'end' "Loading... Please wait.")}
	 {InputText bind(event:"<Control-s>" action:proc{$} X in X = {Press} end)} % You can also bind events
   {InputText bind(event:"<Escape>" action:proc{$}{Application.exit 0} end)}
   {InputText bind(event:"<Return>" action:proc{$} X in X = {Press} end)}
	 
            % On lance les threads de lecture et de parsing
	 %SeparatedWordsPort = {NewPort SeparatedWordsStream}
	 %NbThreads = 32
    %InputTextSplit = {String.tokens "the democrats" & }
	 %{LaunchThreads SeparatedWordsPort NbThreads}

	 
	 {InputText set(1:"")}
      end
   end
    % Appelle la procedure principale
   {Main}
end