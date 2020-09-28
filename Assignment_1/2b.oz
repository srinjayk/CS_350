declare fun {FoldR List BinOp Identity}
	   local FoldRAux in
	      fun {FoldRAux List BinOp Identity}
		 case List
		 of nil then Identity
		 [] H|T then {BinOp H {FoldRAux T BinOp Identity}}
		 end
	      end
	      {FoldRAux List BinOp Identity}
	   end
	end



declare fun {Map List Func}
	   local MapAux in
	      fun {MapAux List Func}
		 case List of nil then nil
		 [] H|T then {FoldR [H] fun{$ X 0}{Func X} end 0} | {MapAux T Func}
		 end
	      end
	      {MapAux List Func}
	   end
	end



{Browse {Map [0 1 2 3 5 58 55] 
               fun {$ X} X*X+X end 
               }}