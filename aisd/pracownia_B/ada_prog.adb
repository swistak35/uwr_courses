with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;

procedure ada_prog is
        m       :integer range 0..1000000;
        n       :integer range 0..1000000;
        rmod    :integer := 999979;
        type myArray is array(Integer range <>) of integer;

        function check_lt(x : CHARACTER) return BOOLEAN;
        function check_lt(x : CHARACTER) return BOOLEAN is
        begin
                return ((x = '1') or else (x = '3') or else (x = '5') or else (x = '7'));
        end check_lt;

        function check_ul(x : CHARACTER) return BOOLEAN;
        function check_ul(x : CHARACTER) return BOOLEAN is
        begin
                return ((x = '2') or else (x = '3') or else (x = '6') or else (x = '7'));
        end check_ul;

        function check_up(x : CHARACTER) return BOOLEAN;
        function check_up(x : CHARACTER) return BOOLEAN is
        begin
                return ((x = '4') or else (x = '5') or else (x = '6') or else (x = '7'));
        end check_up;
begin
        Get(m);
        Get(n);
        declare
                count_tab : myArray(1..(n+1));
                count_pre : myArray(1..(n+1));
                tab : STRING(1..n+1);
                pre : STRING(1..n+1);
        begin
                get(pre);

                count_pre(1) := 1;
                for i in 2..(n+1) loop
                        if (check_lt(pre(i-1))) then
                                count_pre(i) := count_pre(i-1);
                        else
                                count_pre(i) := 0;
                        end if;
                end loop;

                for k in 1..m loop
                        get(tab);

                        for i in 1..(n+1) loop
                                count_tab(i) := 0;

                                if (i /= 1) then
                                        if (check_lt(tab(i-1))) then
                                                count_tab(i) := count_tab(i) + count_tab(i-1);
                                        end if;

                                        if (check_ul(pre(i-1))) then
                                                count_tab(i) := count_tab(i) + count_pre(i-1);
                                        end if;
                                end if;

                                if (check_up(pre(i))) then
                                        count_tab(i) := count_tab(i) + count_pre(i);
                                end if;

                                count_tab(i) := count_tab(i) mod RMOD;
                        end loop;

                        count_pre := count_tab;
                        pre := tab;
                end loop;

                put(Trim(Integer'Image(count_tab(n+1)), Ada.Strings.Left));
        end;
end;