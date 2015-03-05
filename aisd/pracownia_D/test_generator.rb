$ppd = {}

fname = ARGV[0]

$fin = File.open("maxtesty/#{fname}.in", "w")
$fout = File.open("maxtesty/#{fname}.out", "w")


$ppd[:upper] = rand
$ppd[:lower] = rand
$ppd[:delete] = rand
$ppd[:insert] = rand

sum = $ppd[:upper] + $ppd[:lower] + $ppd[:delete] + $ppd[:insert]

$ppd[:upper] /= sum
$ppd[:lower] /= sum
$ppd[:delete] /= sum
$ppd[:insert] /= sum

$ppd[:not_in_set] = rand

puts $ppd

def rand_command
	x = rand
	if rand <= $ppd[:upper]
		'U'
	elsif rand <= $ppd[:upper] + $ppd[:lower]
		'L'
	elsif rand <= $ppd[:upper] + $ppd[:lower] + $ppd[:delete]
		'D'
	else
		'I'
	end
end

n = rand(100) + 999900

$fin.puts(n.to_s)

$tree = []

n.times do
	if $tree.empty? || rand <= $ppd[:not_in_set]
		begin
			x = rand(200)+3
		end while $tree.include? x
	else
		x = $tree.sample
	end
	cmd = rand_command
	cmd = 'D' if $tree.size > 49900 && rand > 0.9
	cmd = 'D' if $tree.size == 50000

	$fin.puts("#{cmd} #{x}")
	case cmd
		when 'U' then 
			res = $tree.select {|y| y >= x}.min
			if res.nil?
				$fout.puts("BRAK")
			else
				$fout.puts(res.to_s)
			end
		when 'L' then
			res = $tree.select {|y| y <= x}.max
			if res.nil?
				$fout.puts("BRAK")
			else
				$fout.puts(res.to_s)
			end
		when 'I' then
			unless $tree.include?(x)
				$tree.push(x)
			end
		when 'D' then
			if $tree.delete(x).nil?
				$fout.puts("BRAK")
			else
				$fout.puts("OK")
			end
	end
end

