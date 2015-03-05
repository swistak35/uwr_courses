with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;

procedure zadaniezero is
	a 	:integer range 0..1000;
	b 	:integer range 0..1000;
	tmp :integer range 0..1000;
begin
	Get(a);
	Get(b);

	if a > b then
		tmp := a;
		a := b;
		b := tmp;
	end if;

	for i in a..b loop
		put(Trim(Integer'Image(i), Ada.Strings.Left));
		new_line;
	end loop;
end;