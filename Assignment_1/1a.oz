declare fun {ReverseList L}
	   local ReverseAux in
	      fun {ReverseAux Remainder Partial}
		 case Remainder
		 of nil
		 then Partial
		 [] H|T then {ReverseAux T H|Partial}
		 end
	      end
	      {ReverseAux L nil}
	   end
	end

declare fun {Take L N}
	   local TakeAux in
	      fun {TakeAux L N Partial}
		 case L of nil
		 then nil
		 else
		    if N =< 0
		    then nil
		    else
		       case L of nil then Partial
		       [] H|T then H|{TakeAux T N-1 Partial}
		       end
		    end
		 end
	      end
	      {TakeAux L N nil}
	   end
	end



{Browse {Take [1 2 3 3 6] 2}}