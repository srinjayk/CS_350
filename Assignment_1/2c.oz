declare fun {FoldL List BinOp Identity}
	   local FoldLAux in
	      fun {FoldLAux List BinOp Identity}
		 case List
		 of nil then Identity
		 [] H|T then {FoldLAux T BinOp {BinOp H Identity}}
		 end
	      end
	      {FoldLAux List BinOp Identity}
	   end
	end


{Browse {FoldL [21 1 2 3 5 58 55] 
               fun {$ X Y} X*Y end 
               1}}