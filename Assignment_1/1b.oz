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

declare
fun {Last L N}
   local LastAux in
      fun {LastAux L N Partial}
	 case L of nil
	 then nil
	 else
	    if N =< 0
	    then nil
	    else
	       case L of nil then Partial
	       [] H|T then H|{LastAux T N-1 Partial}
	       end
	    end
	 end
      end
      {ReverseList {LastAux {ReverseList L} N nil }}
   end
end

{Browse {Last [1 2 3 3 6] 2}}