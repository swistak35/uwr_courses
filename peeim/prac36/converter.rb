lines = File.readlines("Dane.csv")
lines.shift

results = lines.map {|l| l.chomp.gsub(",",".").split(';').map {|x| x.to_f} }

File.open("tabelka.tex", "w") do |f|
  results.each do |r|
    f.puts "#{r[0]} & #{r[2]} & #{r[3]} \\\\"
  end
end
