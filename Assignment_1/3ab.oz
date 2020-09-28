declare fun {Factorial N}
	   local FactorialAux in
	      fun {FactorialAux N}
		 if N >= 1.0
		 then N*{FactorialAux N-1.0}
		 else 1.0
		 end
	      end
	      {FactorialAux N}
	   end
	end

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
	    if N =< 0.0
	    then nil
	    else
	       case L of nil then Partial
	       [] H|T then H|{LastAux T N-1.0 Partial}
	       end
	    end
	 end
      end
      {ReverseList {LastAux {ReverseList L} N nil }}
   end
end

declare fun {Power N X}
	   local PowerAux in
	      fun {PowerAux N X}
		 if N >= 1.0
		 then X*{PowerAux N-1.0 X}
		 else 1.0
		 end
	      end
	      {PowerAux N X}
	   end
	end

declare fun {Sine N Mult X}
	   local SineAux in
	      fun {SineAux N Mult X}
		(Mult*{Power 2.0*N-1.0 X})/{Factorial 2.0*N-1.0}
	      end
	      {SineAux N Mult X}
	   end
	end

declare fun lazy {Taylor X}
	   local TaylorAux in
	      fun {TaylorAux N Mult X}
		 {Sine N Mult X}|{TaylorAux N+1.0 ~Mult X}
	      end
	      {TaylorAux 1.0 1.0 X}
	   end
	end
	
declare fun {Approximate Xs Epsilon}
	   local ApproximateAux in
	      fun {ApproximateAux Xs Epsilon M}
		 case Xs of nil then M
		 []H|T then
		    case T of nil then M
		    []N|O then
		       if {Abs (H-N)} < Epsilon then M+H
		       else {ApproximateAux T Epsilon M+H}
		       end
		    end
		 end
		 
	      end
	      {ApproximateAux Xs Epsilon 0.0}
	   end
	end

{Browse {Approximate {Taylor 0.873} 0.000001}}