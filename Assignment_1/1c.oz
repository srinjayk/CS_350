declare
fun {Merge L M}
   local MergeAux in
      fun {MergeAux L M Partial}
	 case L of nil
	 then
	    case M of nil then Partial
	    []H|T then H|{MergeAux L T Partial}
	    end
	 []J|K then
	    case M of nil then J|{MergeAux K M Partial}
	    [] H|T then
	       if H>J
	       then J|{MergeAux K M Partial}
	       else H|{MergeAux L T Partial}
	       end
	    end
	 end
      end
      {MergeAux L M nil}
   end
end

	 
{Browse {Merge [1 2 3 3 6 100 200 65486452] [1 2 5 5 9 10 11 16 85]}}