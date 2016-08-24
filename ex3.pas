(* example program 3; this is not a coherent program, just test code *)

const x = 3, y = 4;
var z;

begin
	z := 3;
	do z:= z+1 while z < 10;
	write z;
	read z;
	write z;
	write x;
	if y = 4 then
		z := 3
	else 
		write 99;
end.
