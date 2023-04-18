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
   InputTextSplit % = {List.map {String.tokens InputText & } Lower}
   %%% Pour ouvrir les fichiers
   class TextFile
      from Open.file Open.text
   end

   proc {Browse Buf}
      {Browser.browse Buf}
   end
   
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
   fun {Press}
      % TODO
      0
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

   % fun {InsertWord Word Probability Tree}
   %    local InsertedTree Pre Post in 
   %       InsertedTree = tree(Word: tree(Probability))
   %       {Record.partition Tree fun{$ T} T.1 < Probability end Pre Post}
   %       {Adjoin {Adjoin Pre InsertedTree} Post}
   %    end
   % end

   % fun {ParseLine Line X Acc}
   %    case Line of 
   %       nil then X = nil Acc
   %       [] H|T then G in X = {List.map {String.tokens {List.map H Char.toLower} & } String.toAtom} {ParseLine T G Acc|G}
   %    end
   % end

   fun {Lower Word}
       {List.map Word Char.toLower}
   end

   % fun {ParseLines Line}
   %    {List.map {List.filter {List.map {String.tokens Line & } Lower} fun{$ W} W\=nil end} String.toAtom}
   % end

   % fun {FileToTree File X Acc}
   %    local L in
   %       {File getS(L)}
   %       if L == false then X = nil Acc 
   %       else G A in
   %          X = {List.map {List.filter {String.tokens L &.} fun{$ W} W\=nil end} ParseLines}
   %          {Browse X}
   %          {FileToTree File G Acc|G}
   %       end
   %    end
   % end

   % fun {Fold X Y}
   %    if X.1 == Y.1 then 
   %       tree(X X.2 Y.2)
   %    else
   %       X|Y
   %    end
   % end

   % fun {ParseFile File} 
   %    local G Trees A in 
   %       Trees = {FileToTree File G G}.1
   %       A = {FoldL Trees.2 Fold Trees.1}
   %       {Browse A}
   %       for Tree in Trees do
   %          skip
   %       end
   %    end
   %    0
   % end    

   fun {ParseLine Line InputTextSplit}
      case Line#InputTextSplit 
      of nil#nil then nil
      [] (A|B)#(C|D) then if A == C then {ParseLine B D} else {ParseLine Line D} end
      [] (H|T)#nil then H 
      [] nil#(H|T) then nil
      else {Browse {List.map Line String.toAtom}} nil
      end
   end

   fun {UpdatePredicTree Struct Predict}
      local Value PredictTree NewTree in 
         Value = {CondSelect Struct {String.toAtom Predict} 0}
         PredictTree = {MakeRecord tree [{String.toAtom Predict}]}
         PredictTree.{String.toAtom Predict} = Value + 1
         {Adjoin Struct PredictTree}
      end
   end

   fun {ParseFile File Line Struct} 
      local B L Predict Value PredictTree NewTree in 
         Predict = {ParseLine {List.map {String.tokens Line & } Lower} InputTextSplit}
         NewTree = {UpdatePredicTree Struct Predict}

         {File atEnd(B)}
         if B then NewTree
         else 
            {File getS(L)} 
            {ParseFile File L NewTree}
         end
      end
   end

   fun {LaunchTask Files Struct}
      if Files == nil then Struct 
      else Path File  Output Line in 
         Path = {VirtualString.toAtom {GetSentenceFolder}#"/"#Files.1}
         File = {New TextFile init(name:Path flags:[read])}
         {File getS(Line)}
         Output = {ParseFile File Line Struct}
         {LaunchTask Files.2 Output}
      end
   end

   fun  {GetFromTo Original From To Current Final}
      if Current < From then {GetFromTo Original.2 From To Current+1 Final} 
      elseif Current >= To then Final 
      else {GetFromTo Original.2 From To Current+1 {Append Final [Original.1]}}
      end
   end

    %%% Lance les N threads de lecture et de parsing qui liront et traiteront tous les fichiers
    %%% Les threads de parsing envoient leur resultat au port Port
   proc {LaunchThreads Port N}
      % TODO
      local Files FilePerThread in 
         Files = {OS.getDir {GetSentenceFolder}}
         FilePerThread = {FilesAmount Files} div N
         for I in 1..1 do 
            local Tree FPT in 
               Tree = tree()
               if I == N-1 then FPT = FilePerThread + {FilesAmount Files} mod N else FPT = FilePerThread end
               %thread 
                  local R A in 
                     A = {GetFromTo Files I*FilePerThread I*FilePerThread+FPT I*FilePerThread {MakeList 0}}
                     {Browse {List.map A String.toAtom}}
                     R = {LaunchTask A Tree} 
                     {Browse R} 
                     skip
                  end 
               %end
            end
         end
      end
     % for A in 1..N do
       %     if a == N-1 then FPT = FilePerThread + {FilesAmount {OS.getDir {GetSentenceFolder}}} mod N else FilePerThread end

          %thread
             
            %for Name in {OS.getDir {GetSentenceFolder}} do
               
         %       local Name File Text Path Trees Output Line in
         %          Name = {OS.getDir {GetSentenceFolder}}.1
         %          Path = {VirtualString.toAtom {GetSentenceFolder}#"/"#Name}
         %          File = {New TextFile init(name:Path flags:[read])}
         %          {File getS(Line)}
         %          Trees = tree()
         %          Output = {ParseFile File Line Trees}
         %          {Browse Trees}
         %       end
         %    %end 
         % {Browse {String.toAtom "fini"}}
         % skip
     % end
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
       
      local NbThreads Description Window SeparatedWordsStream SeparatedWordsPort in
	 {Property.put print foo(width:1000 depth:1000)}  % for stdout siz
	 
            % TODO
	 
            % Creation de l interface graphique
	 Description=td(
			title: "Text predictor"
			lr(text(handle:InputText width:50 height:10 background:white foreground:black wrap:word) button(text:"Predict" width:15 action:Press))
			text(handle:OutputText width:50 height:10 background:black foreground:white glue:w wrap:word)
			action:proc{$}{Application.exit 0} end % quitte le programme quand la fenetre est fermee
			)
	 
            % Creation de la fenetre
	 Window={QTk.build Description}
	 {Window show}
	 
	 {InputText tk(insert 'end' "Loading... Please wait.")}
	 {InputText bind(event:"<Control-s>" action:Press)} % You can also bind events
	 
            % On lance les threads de lecture et de parsing
	 SeparatedWordsPort = {NewPort SeparatedWordsStream}
	 NbThreads = 32
      InputTextSplit = {String.tokens "the democrats" & }
	 {LaunchThreads SeparatedWordsPort NbThreads}

	 
	 {InputText set(1:"")}
      end
   end
    % Appelle la procedure principale
   {Main}
end