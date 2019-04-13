type
	vector = integer;



var
	n: array [1..10] of integer;
    b, ret : vector;
   a, c : vector;
   
function max (num1, num2: integer): integer;
var
   a: integer;
begin
   if (num1 > num2) then
      result := num1
   
   else
      result := num2;
   max := result;
end;	


begin
   a := 100;
   b := 20;
   ret := max(a ,b);
   
   writeln( 'Max value is : ', ret );
end.
