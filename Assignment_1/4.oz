declare
fun {Line L I}
   local LineAux in
      fun {LineAux L I J}
	 case L of nil then true
	 [] H|T then
	    if I\=J then
	       if H==0 then {And true {LineAux T I J+1}} else {And false {LineAux T I J+1}}
	       end
	    else {And true {LineAux T I J+1}}
	    end
	 end
      end
      {LineAux L I 1}
   end
end


declare
fun {IsDiagonal L}
   local DiagonalAux in
      fun {DiagonalAux L I}
	 case L of nil then true
	 []H|T then {And {Line H I} {DiagonalAux T I+1}}
	 end
      end
      {DiagonalAux L 1}
   end
end


{Browse {IsDiagonal [[1 0 0 0][0 1 0 1][0 0 1 0][0 0 0 1]] }}