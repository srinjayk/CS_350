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

% {Browse {Take [1 2 3 3 6] 2}}

% declare fun {AddStreams L M} 
%     (L.1 + M.1) | {AddStreams L.2 M.2}
% end



declare fun {Find L X}
	local FindAux in
		fun {FindAux L X}
			case L of nil then false
			else 
				if (L.1 == X) then true
				else {And true {Find L.2 X}}
				end
			end
		end
		{FindAux L X}
	end
end

% {Browse {Find [1 2 2] 3}}


declare fun {Unique L}
	local UniqueAux in 
		fun {UniqueAux L Partial}
			case L of nil then Partial
			else
				if {Find Partial L.1}==false then {UniqueAux L.2 L.1|Partial}
				else {UniqueAux L.2 Partial}
				end
			end
		end
		{ReverseList {UniqueAux L nil}}
	end
end

{Browse {Unique [1 2 23 3 3 3 3 34 45]}}

declare fun {Merge L M}
	local MergeAux in
		fun {MergeAux L M Partial}
			case L of nil then Partial
			else 
				case M of nil then Partial
				else 
					if {Find Partial [L.1 M.1]}==false then {MergeAux L.2 M.2 [L.1 M.1]|Partial}
					else {MergeAux L.2 M.2 Partial}
					end
				end
			end
		end
		{ReverseList {MergeAux L M nil}}
	end
end

{Browse {Merge [1 2 3 1] [1 2 3 1]}}