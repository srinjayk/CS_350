declare fun {ZipWith BinOp ListL ListM}
	   local ZipWithAux in
	      fun {ZipWithAux BinOp ListL ListM}
		 case ListL
		 of nil then nil
		 [] H|T then
		    case ListM
		    of nil then nil
		    [] N|O then {BinOp H N}|{ZipWithAux BinOp T O}
		    end      
		 end
	      end
	      {ZipWithAux BinOp ListL ListM}
	   end
	end


{Browse {ZipWith fun {$ X Y} X+Y end 
			[0 1 2 3 5 58 55] [1 2 3 5 6 8 5] 
	}}