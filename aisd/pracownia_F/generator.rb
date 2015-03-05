
$amount = 1
$n = 1000
$m = 1000
$maxval = 1000000000
$req_amount = 100

def fill(tab, row, col)
	if tab[row][col] == 1
		tab[row][col] = 0
		fill(tab, row, col-1) if col > 0
		fill(tab, row, col+1) if col < m-1
		fill(tab, row-1, col) if row > 0
		fill(tab, row+1, col) if row < n-1
	end
end

def find_solution(tab, x)
	tab2 = tab.clone
	tab2.map do |row|
		row.map do |v|
			(v <= x) ? 0 : 1
		end
	end
	
	res = 0
	$n.times do |i|
		$m.times do |j|
			if tab2[i][j] == 1
				fill(tab2, i, j)
				res += 1
			end
		end
	end
	res
end

$amount.times do |test_number|
	tab = Array.new($n) { Array.new($m) }
	$n.times do |i|
		$m.times do |j|
			tab[i][j] = rand($maxval) + 1;
		end
	end

	requests = []
	$req_amount.times do
		requests.push(rand($maxval+2))
	end
	requests.sort!
	results = requests.map {|x| puts "Szukam..."; find_solution(tab, x) }

	File.open("testy/testg#{test_number}.in", "w") do |f|
		f.puts "#{$n} #{$m}"
		$n.times do |i|
			f.puts tab[i].join(" ")
		end
		f.puts "#{$req_amount}"
		f.puts requests.join(" ")
	end

	File.open("testy/testg#{test_number}.out", "w") do |f|
		f.puts results.join(" ")
	end
end

