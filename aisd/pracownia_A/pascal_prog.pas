program ZadanieA;
uses crt, math;
var
    total, mx, half, dist : int64;
    n, i, a, b : longint;
    tab : array of int64;

begin
    total := 0;

    readln(n);
    setlength(tab, n);


    for i := 0 to n-1 do begin
        readln(tab[i]);
        total := total + tab[i];
    end;
    half := total div 2;

    a := 0;
    b := 0;
    dist := 0;
    mx := 0;

    while (b < n) do begin
        if (dist <= half) then begin
            dist := dist + tab[b];
            b := b + 1;
            mx := max(mx, min(dist, total - dist));
        end;

        if (dist > half) then begin
            dist := dist - tab[a];
            a := a + 1;
            mx := max(mx, min(dist, total - dist));
        end;
    end;
    writeln(mx);
end.
